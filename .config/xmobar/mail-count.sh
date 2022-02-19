#!/bin/bash

mailcount=$(mu find maildir:/INBOX | wc -l)

echo "${mailcount}"
