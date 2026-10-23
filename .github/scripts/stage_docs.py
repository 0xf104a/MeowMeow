"""
Stages the documentation site sources: copies the pages the site is built from into `_docs/`, the `docs_dir` of
`zensical.toml`, so that the Markdown files stay where GitHub renders them and the site gets the same pages.

README.md becomes the landing page `index.md`, `docs/` is copied as the `docs/` section and ARCHITECTURE.svg is
copied to the site root so that the relative image links of the architecture page keep working. Two kinds of links
are rewritten in the copies only: links from `docs/` up to README.md point to the landing page, and README links to
repository files through GitHub point to the pages in the site.

Run from any directory: `python .github/scripts/stage_docs.py`.
"""
import re
import shutil
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
STAGE = ROOT / "_docs"
GITHUB_FILE = re.compile(r"https://github\.com/0xf104a/MeowMeow/(?:blob|tree)/master/((?:docs/)?[\w./-]+\.md)")
README_LINK = re.compile(r"((?:\.\./)+)README\.md")


def stage() -> None:
    if STAGE.exists():
        shutil.rmtree(STAGE)
    STAGE.mkdir()
    shutil.copytree(ROOT / "docs", STAGE / "docs", ignore=shutil.ignore_patterns("requirements.txt"))
    for page in (STAGE / "docs").rglob("*.md"):
        page.write_text(README_LINK.sub(r"\1index.md", page.read_text(encoding="utf-8")), encoding="utf-8")
    readme = (ROOT / "README.md").read_text(encoding="utf-8")
    (STAGE / "index.md").write_text(GITHUB_FILE.sub(r"\1", readme), encoding="utf-8")
    shutil.copy(ROOT / "ARCHITECTURE.svg", STAGE / "ARCHITECTURE.svg")


if __name__ == "__main__":
    stage()
    print(f"staged {sum(1 for _ in STAGE.rglob('*') if _.is_file())} files into {STAGE.relative_to(ROOT)}")
