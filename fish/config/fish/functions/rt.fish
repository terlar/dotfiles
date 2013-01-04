function rt --description 'Generate ctags for bundled project'
  ctags --language-force=ruby --exclude=.git --exclude=log -R * (bundle show --paths)
end
