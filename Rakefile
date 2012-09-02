require 'rake'

def file_target(linkable)
  file = linkable.split('/').last

  if file[0] == '_'
    file = linkable.gsub(/\/_/, '/')
  end

  file = ".#{file}"
  "#{ENV['HOME']}/#{file}"
end

def linkables
  Dir['*/**']
end

def link_file(source, target)
  source = File.expand_path(source)
  dirname = File.dirname(target)

  FileUtils.mkpath(dirname) unless File.directory? dirname
  File.symlink(source, target)
end

def update_submodules
  `git submodule init`
  `git submodule update`
end

desc "Pull latest dotfiles and update submodules"
task :update do
  puts "Update dotfiles..."
  `git pull`

  puts "Update submodules..."
  update_submodules
end

desc "Initialize submodules and link dotfiles into home dir"
task :install do
  puts "Get submodules..."
  update_submodules

  skip_all = false
  overwrite_all = false
  backup_all = false

  puts "Link dotfiles..."
  linkables.each do |linkable|
    overwrite = false
    backup = false
    target = file_target(linkable)

    if File.exists?(target) || File.symlink?(target)
      unless skip_all || overwrite_all || backup_all
        puts "File already exists: #{target}, what do you want to do?"
        puts "- [s]kip, [S]kip all"
        puts "- [o]verwrite, [O]verwrite all"
        puts "- [b]ackup, [B]ackup all"
        case STDIN.gets.chomp
        when 'o' then overwrite = true
        when 'b' then backup = true
        when 'O' then overwrite_all = true
        when 'B' then backup_all = true
        when 'S' then skip_all = true
        when 's' then next
        end
      end

      next if skip_all
      FileUtils.rm_rf(target) if overwrite || overwrite_all
      FileUtils.mv(target, "#{target}.backup") if backup || backup_all
    end

    puts "Linking #{target}"
    link_file(linkable, target)
  end
end

desc "Remove linked dotfiles from home dir and restore backups"
task :uninstall do
  linkables.each do |linkable|
    target = file_target(linkable)

    # Remove all symlinks created during installation
    if File.symlink?(target)
      puts "Unlink #{target}"
      FileUtils.rm(target)
    end

    # Replace any backups made during installation
    if File.exists?("#{target}.backup")
      puts "Restore #{target}"
      FileUtils.mv("#{target}.backup", target)
    end
  end
end
