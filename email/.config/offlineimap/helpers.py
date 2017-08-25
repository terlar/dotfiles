#!/usr/bin/env python
import json
import os
import sys

mapping = {
    "INBOX":              "inbox",
    "[Gmail]/All Mail":   "archive",
    "[Gmail]/Drafts":     "drafts",
    "[Gmail]/Important":  "important",
    "[Gmail]/Sent Mail":  "sent",
    "[Gmail]/Spam":       "spam",
    "[Gmail]/Starred":    "starred",
    "[Gmail]/Trash":      "trash",
    "Economy":            "economy",
    "Cultivation":        "cultivation",
    "Entertainment":      "entertainment",
    "Life":               "life",
    "Chinese":            "chinese"
}
r_mapping = {val: key for key, val in mapping.items()}


def nt_remote(folder):
    return mapping.get(folder, folder)


def nt_local(folder):
    return r_mapping.get(folder, folder)


def exclude(excludes):
    return lambda folder: folder not in excludes


def get_client_id():
    try:
        return os.environ["CLIENT_ID"]
    except KeyError:
        print("Please set the environment variable CLIENT_ID")
        sys.exit(1)


def get_client_secret():
    try:
        return os.environ["CLIENT_SECRET"]
    except KeyError:
        print("Please set the environment variable CLIENT_SECRET")
        sys.exit(1)


def get_username(account):
    file = os.path.expanduser(
        "~/.cache/offlineimap/google-token-{}.json".format(account))
    with open(file) as data_file:
        data = json.load(data_file)
    return data["username"]


def get_access_token(account):
    file = os.path.expanduser(
        "~/.cache/offlineimap/google-token-{}.json".format(account))
    with open(file) as data_file:
        data = json.load(data_file)
    return data["access_token"]


def get_refresh_token(account):
    file = os.path.expanduser(
        "~/.cache/offlineimap/google-token-{}.json".format(account))
    with open(file) as data_file:
        data = json.load(data_file)
    return data["refresh_token"]
