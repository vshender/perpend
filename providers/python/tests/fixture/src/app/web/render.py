from app.core.models import Model


class Renderer:
    def render(self, model: Model) -> str:
        return model.dump()
