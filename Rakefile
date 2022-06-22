# frozen_string_literal: true

require 'bundler/gem_tasks'
require 'rake/testtask'
require 'rubocop/rake_task'

Rake::TestTask.new :test do |task|
  task.libs << 'test'
  task.libs << 'lib'
  task.test_files = FileList['test/**/test_*.rb']
end

RuboCop::RakeTask.new

task default: %i[test rubocop]
