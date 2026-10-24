require "testapi/http.rb"
require "testapi/file.rb"
require "digest"

$testroot = ""

if ENV["TESTROOT"].nil? then
   puts "TESTS WARNING: TESTROOT is not set. Please use `export TESTROOT=<path to directory with vsn.txt>` to set it."
else
   $testroot = ENV["TESTROOT"]
end

module Tests
  # vsn.txt holds the bare release version; the server appends the commit it
  # was built from: MeowMeow/<version>.<sha>[.dirty] (see scripts/git_vsn.sh).
  def self.get_vsn
      vsn = File.read(File.join($testroot, "vsn.txt"))
      "MeowMeow/"+vsn.strip
  end

  def self.test_server_header
      r = TestHTTP::get("http://localhost")
      vsn = Regexp.escape(self.get_vsn)
      !(r.headers["Server"] =~ /\A#{vsn}\.(?:[0-9a-f]+|unknown)(\.dirty)?\z/).nil?
  end

  def self.test_big_file
      r = TestHTTP::get("http://localhost/FatCat")
      print "#{r.duration}s..."
      Digest::MD5.hexdigest(r.data) == FileAPI::md5("/var/www/FatCat") and r.duration > 0.020
  end
  
end
