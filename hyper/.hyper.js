module.exports = {
  config: {
    fontSize: 18,
    fontFamily: '"Fantasque Sans Mono", monospace',

    cursorColor: '#2c3e50',
    cursorShape: 'BEAM',
    cursorBlink: false,

    foregroundColor: '#263238',
    backgroundColor: '#eee',
    borderColor: '#2c3e50',

    css: '',
    termCSS: '',

    showHamburgerMenu: false,
    showWindowControls: false,

    padding: '1em',
    colors: {
      black: '#263238',
      red: '#f44336',
      green: '#4caf50',
      yellow: '#ff9800',
      blue: '#2196f3',
      magenta: '#ab47bc',
      cyan: '#26a69a',
      white: '#b0bec5',
      lightBlack: '#607d8b',
      lightRed: '#ef5350',
      lightGreen: '#00c853',
      lightYellow: '#bcaaa4',
      lightBlue: '#42a5f5',
      lightMagenta: '#ba68c8',
      lightCyan: '#4db6ac',
      lightWhite: '#eeeeee'
    },

    shell: '',
    shellArgs: ['--login'],

    env: {},

    bell: 'SOUND',
    copyOnSelect: true,
  },
  plugins: [
    'hyperminimal',
    'hyperterm-alternatescroll',
    'hyperlinks',
    'hyper-pane',
    'hyper-simple-highlight-active-session',
  ],
  localPlugins: [],

  alternateScroll: {
    scrollSpeed: 100,
  },
};
