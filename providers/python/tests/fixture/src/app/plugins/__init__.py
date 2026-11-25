from importlib import import_module


def load(name: str):
    if not name:
        return import_module("app.plugins.missing")
    return import_module(name)
