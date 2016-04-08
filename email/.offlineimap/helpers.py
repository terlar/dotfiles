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
