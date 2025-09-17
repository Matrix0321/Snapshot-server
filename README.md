# Snapshot Server

Snapshot-Based Relational Data Server & CLI, built with **Haskell**, **Servant**, and **SQLite**.  
It allows administrators to create **many-sorted relation snapshots** from Wikidata SPARQL queries, store them locally, and serve them for students or other applications.

---

##  Features

- Create snapshots from **Wikidata SPARQL queries**
- Support for **many-sorted structures**:
  - Multiple **carrier sets** (e.g., `Person`, `Place`, `Year`)
  - Multiple **relations** with domain/range sorts (e.g., `born : Person <-> Year`)
- Serve stored snapshots via REST API
- Inspect **snapshot signatures** (sorts and relations)
- CLI interface for quick interaction
- Minimal static web UI (served from `/static`)

# Snapshot Server

Snapshot-Based Relational Data Server & CLI, built with **Haskell**, **Servant**, and **SQLite**.  
It allows administrators to create **many-sorted relation snapshots** from Wikidata SPARQL queries, store them locally, and serve them for students or other applications.

---

## Features

- Create snapshots from **Wikidata SPARQL queries**
- Support for **many-sorted structures**:
  - Multiple **carrier sets** (e.g., `Person`, `Place`, `Year`)
  - Multiple **relations** with domain/range sorts (e.g., `born : Person <-> Year`)
- Serve stored snapshots via REST API
- Inspect **snapshot signatures** (sorts and relations)
- CLI interface for quick interaction
- Minimal static web UI (served from `/static`), supporting create, browse, inspect, download, and delete

---

## Project Structure

```bash
snapshot-server/
├── app/                     # Main server entrypoint
│   └── Main.hs
├── app-cli/                 # CLI entrypoint
│   └── Main.hs
├── docs/                    # Documentation (report, diagrams, etc.)
├── examples/                # Example payloads
│   └── sampleInput.json
├── src/                     # Haskell source files
│   ├── API.hs
│   ├── DB.hs
│   ├── Error.hs
│   ├── Main.hs
│   ├── Models.hs
│   ├── Server.hs
│   ├── SnapshotGenerator.hs
│   ├── Spec.hs
│   ├── Types.hs
│   └── Validation.hs
├── static/                  # Browser-based UI
│   └── index.html
├── .gitignore
├── CHANGELOG.md
├── LICENSE
├── README.md
├── Setup.hs
├── snapshot-api.json         # OpenAPI spec
├── snapshot-server.cabal     # Cabal package description
├── stack.yaml
├── stack.yaml.lock
├── curl_commands.sh          # Example curl script
├── config.json               # CLI/server config
├── init.sql                  # Initial DB schema
├── snapshots.db              # SQLite DB (generated)
└── test.db                   # Test database
```
---

##  Getting Started

### Requirements
- **GHC ≥ 9.8**
- **Stack ≥ 2.11**
- **SQLite3** 

### 1. Build & Run
```bash
stack build
stack run
```

The server will start on:
- API: http://localhost:8080
- UI: http://localhost:8080/index.html

### 2. Run Example Requests

A ready-made `examples/sampleInput.json` (from Wikidata) is already provided.
To quickly test all endpoints, run:
```bash
bash curl_commands.sh
```
This script will:
- Create a snapshot (`POST /snapshots`)
- List all snapshots (`GET /snapshots`)
- Retrieve a snapshot by name (`GET /snapshots/:name`)
- Retrieve a snapshot signature (`GET /snapshots/:name/signature`)
- Delete a snapshot (`DELETE /snapshots/:name`)
- Delete all snapshots (`DELETE /snapshots`)

## Example Payload



`examples/sampleInput.json`

```json
{
  "snapshotName": "CountryCapitals",
  "sortSymbols": ["countryLabel"],
  "relations": [
    {
      "relationName": "CountryCapitals",
      "tuples": [
        { "countryLabel": "Canada", "capitalLabel": "Ottawa" },
        { "countryLabel": "Germany", "capitalLabel": "Berlin" },
        { "countryLabel": "Japan", "capitalLabel": "Tokyo" }
      ]
    }
  ]
}
```
Note: Although JSON encodes relations as arrays for compatibility with web clients, internally they are interpreted as sets (order immaterial, no duplicates).

## Command-Line Interface (CLI) Usage
Besides the HTTP server, a CLI tool is also provided.

```bash
# Start CLI
stack exec snapshot-server-cli

# List snapshot names (for use with `get`)
stack exec snapshot-server-cli list

# Get one snapshot with pretty JSON
stack exec snapshot-server-cli get CountryCapitals

# Create snapshot from a JSON file
stack exec snapshot-server-cli create examples/sampleInput.json
```

## SPARQL-like Query Language

- Syntax is inspired by SPARQL but simplified.
- Only supports `SELECT` queries with `WHERE` blocks.
- No full join semantics; instead, results are flattened into named relations.
- Differences from SPARQL:

  - Only basic triple patterns supported.
  - Optional clauses and filters are not yet implemented.
  - Results must map directly into a snapshot relation.
A more complete specification is included in the report.






