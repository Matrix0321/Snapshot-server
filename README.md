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

---

##  Getting Started

### Requirements
- **GHC ≥ 9.8**
- **Stack ≥ 2.11**
- **SQLite3** installed locally

### Build
```bash
stack build

