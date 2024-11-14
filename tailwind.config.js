const defaultTheme = require('tailwindcss/defaultTheme')

module.exports = {
  content: [
  './posts/**/*.{html, md}',
  './templates/**/*.{html, md}',
  '*.{html, md}',
  ],
  darkMode: 'class',
  theme: {
    extend: {
      fontFamily: {
        sans: [
          'Inter',
          ...defaultTheme.fontFamily.sans,
        ],
        serif: [
          'Roboto Slab',
          ...defaultTheme.fontFamily.serif
        ],
      },
      // I CALL THIS THEME iffy theme
      colors: {
        primary: '#B8D876',
        secondary: '#D8CE76',
        terciary: '#D87676',
        base1: '#131311',
        base2: '#1E1E1A',
        base3: '#2C2C25',
        current: '#EFFECF',
        error:  '#D87676',
        warn: '#D8B176'
      },
    },
  },
  variants: {},
  plugins: [],
}