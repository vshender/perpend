from . import models
from .models import Model

try:
    import yaml
except ImportError:
    yaml = None


def load(text: str) -> Model:
    data = yaml.safe_load(text) if yaml else {"name": text}
    return models.Model(data["name"])
