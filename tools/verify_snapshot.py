"""Offline checks only. Does not import or execute archived production scripts."""
from pathlib import Path
import ast, hashlib, json, re, sys

ROOT = Path(__file__).resolve().parents[1]
TOKEN_PATTERNS = [
    re.compile(r'\b(?:AKIA|ASIA)[A-Z0-9]{16}\b'),
    re.compile(r'\b(?:gh[pousr]_[A-Za-z0-9]{20,}|github_pat_[A-Za-z0-9_]{20,})'),
    re.compile(r'\b\d{8,12}:[A-Za-z0-9_-]{30,}\b'),
    re.compile(r'-----BEGIN [^-]*PRIVATE KEY-----'),
]
errors=[]
syntax_notes=[]
manifest=json.loads((ROOT/'inventory/source-manifest.json').read_text(encoding='utf-8'))
for row in manifest:
    path=ROOT/row['repository_path']
    if not path.is_file(): errors.append({'path':row['repository_path'],'issue':'missing'});continue
    raw=path.read_bytes()
    if hashlib.sha256(raw).hexdigest()!=row['copy_sha256']:
        errors.append({'path':row['repository_path'],'issue':'hash mismatch'})
    if path.suffix.lower()=='.docx':continue
    text=raw.decode('utf-8-sig')
    if any(p.search(text) for p in TOKEN_PATTERNS):errors.append({'path':row['repository_path'],'issue':'credential pattern'})
    if path.suffix.lower()=='.ipynb':
        nb=json.loads(text)
        if any(c.get('outputs') or c.get('attachments') for c in nb['cells']):errors.append({'path':row['repository_path'],'issue':'notebook payload'})
    if path.suffix.lower()=='.py':
        try:ast.parse(text)
        except SyntaxError as exc:syntax_notes.append({'path':row['repository_path'],'line':exc.lineno,'issue':'pre-existing or copied Python syntax requires review'})
print(json.dumps({'manifest_files':len(manifest),'integrity_or_secret_errors':errors,'python_syntax_notes':syntax_notes},indent=2))
sys.exit(1 if errors else 0)
