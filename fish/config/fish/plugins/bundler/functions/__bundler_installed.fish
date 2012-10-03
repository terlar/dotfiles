function __bundler_installed --description 'Check if bundler is installed'
  which bundle >/dev/null 2>&1
end
