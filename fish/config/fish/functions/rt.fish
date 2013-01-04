function rt --description 'Generate ctags for bundled project'
  ctags --extra=+f --language-force=ruby -R (bundle show --paths) app lib
end
