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
        terciary: '#76D8A3',
        base1: '#1E1E1A',
        base2: '#2C2C25',
        base3: '#586049',
        current: '#EFFECF',
        error:  '#D87676',
        warn: '#D8B176'
      },
    },
  },
  variants: {},
  plugins: [],
}