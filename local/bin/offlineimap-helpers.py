#!/usr/bin/env python
import os, re, subprocess

mapping = { 'INBOX':              'inbox'
          , '[Gmail]/All Mail':   'archive'
          , '[Gmail]/Drafts':     'drafts'
          , '[Gmail]/Important':  'important'
          , '[Gmail]/Sent Mail':  'sent'
          , '[Gmail]/Spam':       'spam'
          , '[Gmail]/Starred':    'starred'
          , '[Gmail]/Trash':      'trash'
          , 'Economy':            'economy'
          , 'Cultivation':        'cultivation'
          , 'Entertainment':      'entertainment'
          , 'Life':               'life'
          , 'Chinese':            'chinese'
          }

r_mapping = { val: key for key, val in mapping.items() }

def nt_remote(folder):
    return mapping.get(folder, folder)

def nt_local(folder):
    return r_mapping.get(folder, folder)

# folderfilter = exclude(['Label', 'Label', ... ])
def exclude(excludes):
    return lambda folder: not folder in excludes

def get_credentials(account, meta=None):
    out = subprocess.check_output(['pass', 'show', 'mail/%s' % account], universal_newlines=True)
    lines = str(out).strip().splitlines()
    if meta is None:
        return lines[0]
    else:
        regex = re.compile('^%s: (.*)' % meta)
        matches = (m.group(1) for l in lines for m in [regex.search(l)] if m)
        return next(matches, None)
