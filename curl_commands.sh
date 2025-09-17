#!/usr/bin/env bash
# Quick test script for snapshot-server API
# Run with: bash curl_commands.sh

SERVER=http://localhost:8080

echo "=== Create snapshot from sampleInput.json ==="
curl -X POST $SERVER/snapshots \
  -H "Content-Type: application/json" \
  -d @sampleInput.json
echo -e "\n"

echo "=== List all snapshots ==="
curl -X GET "$SERVER/snapshots?page=1&limit=10"
echo -e "\n"

echo "=== Get snapshot by name (CountryCapitals) ==="
curl -X GET $SERVER/snapshots/CountryCapitals
echo -e "\n"

echo "=== Get snapshot signature ==="
curl -X GET $SERVER/snapshots/CountryCapitals/signature
echo -e "\n"

echo "=== Delete snapshot by name (CountryCapitals) ==="
curl -X DELETE $SERVER/snapshots/CountryCapitals
echo -e "\n"

echo "=== Delete all snapshots ==="
curl -X DELETE $SERVER/snapshots
echo -e "\n"

echo "Done."
