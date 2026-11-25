from app.core.models import Model

escape = __import__("html").escape


def decorate(model: Model) -> Model:
    return Model(escape(model.name))
