use std::future::Future;
use std::ops::ControlFlow;
use std::pin::Pin;
use std::task::{Context, Poll};

use tokio::sync::oneshot;

use async_lsp::{AnyEvent, AnyNotification, AnyRequest, LspService, ResponseError, Result};
use tower::{Layer, Service};

use crate::server::SettingState;

macro_rules! ready {
    ($e:expr $(,)?) => {
        match $e {
            core::task::Poll::Ready(t) => t,
            core::task::Poll::Pending => return core::task::Poll::Pending,
        }
    };
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum State {
    #[default]
    Ready,
    Loading,
}

#[derive(Debug, Default)]
pub struct LoaderProgress<S> {
    service: S,
    state: State,
    receiver: Option<oneshot::Receiver<()>>,
}

impl<S> LoaderProgress<S> {
    /// Creating the `Lifecycle` middleware in uninitialized state.
    #[must_use]
    pub fn new(service: S) -> Self {
        Self {
            service,
            receiver: None,
            state: State::Ready,
        }
    }
}

impl<S: LspService> Service<AnyRequest> for LoaderProgress<S>
where
    S::Error: From<ResponseError>,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = S::Future;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        match self.state {
            State::Ready { .. } => return Poll::Ready(ready!(self.service.poll_ready(cx))),
            State::Loading => match self.receiver.take() {
                Some(mut receiver) => {
                    if Pin::new(&mut receiver).poll(cx).is_pending() {
                        tracing::info!("rate limit exceeded; sleeping.");
                        return Poll::Pending;
                    }
                }
                None => {}
            },
        }

        self.state = State::Ready;

        Poll::Ready(ready!(self.service.poll_ready(cx)))
    }

    fn call(&mut self, req: AnyRequest) -> Self::Future {
        match self.state {
            State::Ready => self.service.call(req),
            State::Loading => panic!("service not ready; poll_ready must be called first"),
        }
    }
}

impl<S: LspService> LspService for LoaderProgress<S>
where
    S::Error: From<ResponseError>,
{
    fn notify(&mut self, notif: AnyNotification) -> ControlFlow<async_lsp::Result<()>> {
        tracing::info!("{:?}", notif);
        self.service.notify(notif)
    }

    fn emit(&mut self, event: AnyEvent) -> ControlFlow<async_lsp::Result<()>> {
        tracing::info!("Got Event! {:?}", event.type_name());

        match event.downcast::<SettingState>() {
            Ok(SettingState(receiver)) => {
                self.receiver = Some(receiver);
                tracing::info!("Setting Receiver!");
                self.state = State::Loading;
                return ControlFlow::Continue(());
            }
            Err(event) => return self.service.emit(event),
        }
    }
}

#[must_use]
#[derive(Clone, Default)]
pub struct LoaderProgressLayer {
    _private: (),
}

impl<S> Layer<S> for LoaderProgressLayer {
    type Service = LoaderProgress<S>;

    fn layer(&self, inner: S) -> Self::Service {
        LoaderProgress::new(inner)
    }
}
