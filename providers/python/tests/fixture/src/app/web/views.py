import importlib
from typing import TYPE_CHECKING

from app.core import models, storage
from app.core.models import Model
from helpers import slug, text

if TYPE_CHECKING:
    from app.web.render import Renderer


def show(name: str, renderer: "Renderer") -> str:
    from app.plugins import default

    model: Model = storage.load(slug(name))
    return renderer.render(default.decorate(model))


def plugin(name: str):
    try:
        return importlib.import_module("app.plugins." + text.shorten(name))
    except ImportError:
        return importlib.import_module("app.plugins.default")


def kind(model) -> bool:
    return isinstance(model, models.Model)
