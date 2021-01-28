#!/bin/sh

gpg --symmetric --cipher-algo AES256 --passphrase="$VAULT_PASS" "$INPUT_SECRET"