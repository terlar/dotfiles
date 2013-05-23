function rt --description 'Generate ctags for bundled project'
  ctags -R --languages=ruby --exclude=.git
end
