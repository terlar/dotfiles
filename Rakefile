require 'rake'

def file_target(linkable)
  file = linkable.split('/').last

  if linkable == linkable.gsub!(/\/_/, '/')
    file = linkable
  end

  file = ".#{file}"
  "#{ENV["HOME"]}/#{file}"
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

desc "Link dotfiles into home dir."
task :install do
  skip_all = false
  overwrite_all = false
  backup_all = false

  linkables.each do |linkable|
    overwrite = false
    backup = false
    target = file_target(linkable)

    if File.exists?(target) || File.symlink?(target)
      unless skip_all || overwrite_all || backup_all
        puts "File already exists: #{target}, what do you want to do? [s]kip, [S]kip all, [o]verwrite, [O]verwrite all, [b]ackup, [B]ackup all"
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
      `mv "#{target}" "#{target}.backup"` if backup || backup_all
    end

    puts "Linking #{target}"
    link_file(linkable, target)
  end
end

task :uninstall do
  linkables.each do |linkable|
    target = file_target(linkable)

    # Remove all symlinks created during installation
    if File.symlink?(target)
      FileUtils.rm(target)
    end

    # Replace any backups made during installation
    if File.exists?("#{target}.backup")
      `mv "#{target}.backup" "#{target}"`
    end
  end
end
