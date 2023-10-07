use std::future::Future;
use std::ops::ControlFlow;
use std::pin::Pin;
use std::task::{Context, Poll};

use lsp_types::notification;
use lsp_types::notification::Notification;
use tokio::sync::oneshot;

use async_lsp::{AnyEvent, AnyNotification, AnyRequest, LspService, ResponseError, Result};
use tower::{Layer, Service};

use crate::server::SetPackageGraphEvent;

macro_rules! ready {
    ($e:expr $(,)?) => {
        match $e {
            core::task::Poll::Ready(t) => t,
            core::task::Poll::Pending => return core::task::Poll::Pending,
        }
    };
}

#[derive(Debug, Default)]
pub struct LoaderProgress<S> {
    service: S,
    receiver: Option<oneshot::Receiver<()>>,
    sender: Option<oneshot::Sender<()>>,
}

impl<S> LoaderProgress<S> {
    /// Creating the `Lifecycle` middleware in uninitialized state.
    #[must_use]
    pub fn new(service: S) -> Self {
        Self {
            service,
            receiver: None,
            sender: None,
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
        match self.receiver.take() {
            None => return Poll::Ready(ready!(self.service.poll_ready(cx))),
            Some(mut receiver) => {
                if Pin::new(&mut receiver).poll(cx).is_pending() {
                    tracing::info!("Paused! waiting until packages are downloaded");
                    return Poll::Pending;
                }
            }
        }

        Poll::Ready(ready!(self.service.poll_ready(cx)))
    }

    fn call(&mut self, req: AnyRequest) -> Self::Future {
        self.service.call(req)
    }
}

impl<S: LspService> LspService for LoaderProgress<S>
where
    S::Error: From<ResponseError>,
{
    fn notify(&mut self, notif: AnyNotification) -> ControlFlow<async_lsp::Result<()>> {
        if &*notif.method == notification::Initialized::METHOD {
            let (tx, rx) = oneshot::channel();
            self.receiver = Some(rx);
            self.sender = Some(tx);
        }
        self.service.notify(notif)
    }

    fn emit(&mut self, event: AnyEvent) -> ControlFlow<async_lsp::Result<()>> {
        if event.is::<SetPackageGraphEvent>() {
            match self.sender.take() {
                Some(sender) => {
                    let _ = sender.send(());
                }
                None => {
                    panic!("This should not happen")
                }
            }
        }
        self.service.emit(event)
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
