import json
from dataclasses import dataclass


@dataclass
class Model:
    name: str

    def dump(self) -> str:
        return json.dumps({"name": self.name})
