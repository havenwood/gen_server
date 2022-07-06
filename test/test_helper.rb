# frozen_string_literal: true

$VERBOSE = nil
$LOAD_PATH.unshift File.expand_path('../lib', __dir__)

require 'gen_server'
require 'minitest/autorun'
require 'minitest/pride'
