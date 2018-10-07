c.colors.statusbar.command.bg = '#263238'
c.colors.statusbar.normal.bg = '#263238'
c.colors.tabs.selected.even.bg = '#263238'
c.colors.tabs.selected.odd.bg = '#263238'

c.aliases = {
    'w': 'session-save',
    'q': 'quit',
    'wq': 'quit --save',
    'mpv': 'spawn --userscript view_in_mpv'
}

c.auto_save.session = True

c.content.default_encoding = 'utf-8'
c.content.javascript.log = {'unknown': 'none', 'info': 'debug', 'warning': 'debug', 'error': 'debug'}

c.downloads.position = 'bottom'
c.downloads.remove_finished = 1000

c.editor.command = ['emacsclient', '-c', '{}']

c.spellcheck.languages = ['en-US', 'sv-SE']

c.statusbar.padding = { 'top': 5, 'bottom': 5, 'left': 5, 'right': 5 }

c.tabs.background = True
c.tabs.padding = {'top': 5, 'bottom': 5, 'left': 5, 'right': 5}
c.tabs.position = 'left'
c.tabs.show = 'multiple'
c.tabs.title.format = ' {title}'
c.tabs.width = '10%'

c.url.searchengines = {
    'DEFAULT': 'https://duckduckgo.com/?q={}',
    'g': 'https://google.com/search?q={}',
    'gh': 'https://github.com/search?q={}',
    'we': 'https://en.wikipedia.org/w/index.php?search={}',
    'ws': 'https://sv.wikipedia.org/w/index.php?search={}',
    'aw': 'https://wiki.archlinux.org/index.php?search={}',
    'aur': 'https://aur.archlinux.org/packages/?K={}',
    'pak': 'https://archlinux.org/packages/?q={}',
    'h': 'https://www.haskell.org/hoogle/?hoogle={}',
    'hackage': 'https://hackage.haskell.org/packages/search?terms={}'
}
c.url.start_pages = 'https://start.duckduckgo.com'

c.zoom.default = '100%'


c.bindings.key_mappings = {
    '<Ctrl-g>'       : '<Escape>',
    '<Ctrl-[>'       : '<Escape>',
    '<Ctrl-6>'       : '<Ctrl-^>',
    '<Ctrl-m>'       : '<Return>',
    '<Ctrl-j>'       : '<Return>',
    '<Shift-Return>' : '<Return>',
    '<Enter>'        : '<Return>',
    '<Shift-Enter>'  : '<Return>',
    '<Ctrl-Enter>'   : '<Ctrl-Return>',
}

# Emacs bindings
c.bindings.commands = {
    'insert': {
        '<Ctrl-f>'         : 'fake-key <Right>',
        '<Ctrl-b>'         : 'fake-key <Left>',
        '<Ctrl-a>'         : 'fake-key <Home>',
        '<Ctrl-e>'         : 'fake-key <End>',
        '<Ctrl-n>'         : 'fake-key <Down>',
        '<Ctrl-p>'         : 'fake-key <Up>',
        '<Alt-v>'          : 'fake-key <PgUp>',
        '<Ctrl-v>'         : 'fake-key <PgDown>',
        '<Alt-f>'          : 'fake-key <Alt-Right>',
        '<Alt-b>'          : 'fake-key <Alt-Left>',
        '<Ctrl-d>'         : 'fake-key <Delete>',
        '<Alt-d>'          : 'fake-key <Alt-Delete>',
        '<Ctrl-k>'         : 'fake-key <Shift-End> ;; fake-key <Delete>',
        "<Ctrl-c><Ctrl-'>" : 'open-editor',
    }
}

## Bindings for normal mode
config.bind('<Alt-9>', 'tab-focus 9')
config.bind('<Alt-0>', 'tab-focus -1')
config.bind('<Space>', 'set-cmd-text -s :buffer')
config.bind('O', 'set-cmd-text -s :open {url:pretty}')
config.bind('T', 'set-cmd-text :open -t -r {url:pretty}')
config.bind('gt', 'tab-next')
config.bind('gT', 'tab-prev')
config.bind('t', 'set-cmd-text -s :open -t')
config.bind('x', 'spawn --userscript view_in_mpv')
config.bind('X', 'hint links spawn mpv {hint-url}')
