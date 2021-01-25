#!/bin/sh

gpg --quiet --batch --yes --decrypt --passphrase="$VAULT_PASS" \
--output "$RESULTS_OUTPUT" "$RESULTS_INPUT"