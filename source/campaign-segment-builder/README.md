# Campaign Segment Builder

A personal, local Codex skill plugin for Prachi's Campaign Manager segment workflow:

Objective -> ClickHouse SQL review -> authorized validation -> Python generator -> Server 93 deployment -> daily scheduling -> admin Google Sheet and config registration.

## Usage

After installation, invoke Campaign Segment Builder and say:

> Naya campaign segment banana hai. Objective se shuru karo aur query review ke liye do.

Supply the business objective. The skill uses the documented Server 93 locations and checks existing implementation patterns before drafting SQL. It preserves query review and approval before production changes.

## Contents and prerequisites

- `plugin.json`: portable Agent Plugins manifest.
- `.codex-plugin/plugin.json`: Codex compatibility manifest.
- `skills/create-campaign-segment/SKILL.md`: workflow and approval rules.
- Skill references: source provenance, environment knowledge, and completion checklist.

This package contains reusable instructions, not an MCP server, credentials, preconfigured database connection, or background scheduler. Execution requires the host's authorized network/file access, ClickHouse connection, Google Sheets access, and Server 93 scheduling permissions. Missing access is reported without claiming completion.

The package is created locally. It has not been installed or registered in a marketplace. Installation into a personal Codex marketplace is a separate step. Do not upload this environment-specific package publicly without reviewing its internal paths and business context.

## Operational scope

The plugin creates segments and registers them for master inclusion. Campaign master rebuilding and Server 53 publication require a separate request. Creating this plugin does not modify Server 93, execute SQL, update any Google Sheet, or create a scheduled job.

Source: `Campaign master whole process.docx` and the prior "Check network path access" task, inspected 2026-11-17. Historical server findings must be reverified when used.
