# frozen_string_literal: true

require_relative 'lib/gen_server/version'

Gem::Specification.new do |spec|
  spec.name = 'gen_server'
  spec.version = GenServer::VERSION
  spec.authors = ['Shannon Skipper']
  spec.email = ['shannonskipper@gmail.com']

  spec.summary = 'A Ractor-backed GenServer'
  spec.description = 'A GenServer implemented with Ruby Ractors'
  spec.homepage = 'https://github.com/havenwood/gen_server'
  spec.license = 'MIT'
  spec.required_ruby_version = '>= 3.1.0'

  spec.files = Dir.chdir(File.expand_path(__dir__)) do
    `git ls-files -z`.split("\x0").reject do |f|
      (f == __FILE__) || f.match(%r{\A(?:(?:bin|test)/|\.(?:git))})
    end
  end
  spec.executables = spec.files.grep(%r{\Abin/}) { |f| File.basename(f) }
  spec.require_paths = ['lib']
  spec.metadata['rubygems_mfa_required'] = 'true'
end
