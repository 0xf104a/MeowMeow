#!/bin/sh
# External auth tool for Authorize-Via E2E testing
INPUT=$(cat)
if [ "$INPUT" = "secret-token-123" ] || [ "$INPUT" = "Bearer secret-token-123" ]; then
    exit 0
else
    exit 1
fi
