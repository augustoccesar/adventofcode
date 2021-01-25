#!/bin/sh

gpg --quiet --batch --yes --decrypt --passphrase="$VAULT_PASS" \
--output "$INPUT_OUTPUT" "$INPUT_INPUT"