# ruby test file ruby.rb

def File.binread(fname)
  open(fname, 'rb') {|f|
    return f.read
  }

class ConfigTable

  include Enumerable

  def initialize(rbconfig)
    @rbconfig = rbconfig
    @no_harm = false
