require "testapi/http.rb"


module Tests
  def self.test_header_set
    r = TestHTTP::get("http://localhost/")
    r.headers["X-Powered-By"] == "Pusheen the Cat"
  end

  def self.test_ok
    r = TestHTTP::get("http://localhost/")
    r.code == 200
  end

  def self.test_204
    r = TestHTTP::get("http://localhost/generate_204")
    r.code == 204 and r.data == nil
  end

  def self.test_404
    r = TestHTTP::get("http://localhost/abstract_nonsense")
    r.code == 404
  end

  def self.test_abort
    r = TestHTTP::get("http://localhost/i_am_a_teapot")
    r.code == 418
  end
 
  def self.test_different_host
    r = TestHTTP::get("http://127.0.0.1/duckduck")
    r.code == 200 and r.headers["X-Different-Host"] == "true" and r.headers["X-Powered-By"] == "Pusheen the Cat"
  end

  def self.test_code
    r = TestHTTP::get("http://localhost/catnip")
    r.code == 451 and r.data.strip == "Selling catnip to Pusheen the cat is offically prohibited by the cats party." 
  end
 
  def self.test_send_file
    r = TestHTTP::get("http://localhost/catnip_info")
    r.code == 200 and r.headers["Server"] == self.get_vsn and r.data.strip == "Selling catnip to Pusheen the cat is offically prohibited by the cats party."
  end

  def self.test_send_file_non_extistent
    r = TestHTTP::get("http://localhost/get_500")
    r.code == 500
  end

  def self.test_quotes_abort
    r = TestHTTP::get("http://localhost/quotes_418")
    r.code == 418
  end

  def self.test_include
    r = TestHTTP::get("http://localhost/absolute_includable_nonsense")
    r.code == 402
  end

  def self.test_mime
    r = TestHTTP::get("http://localhost/mime.html")
    r.code == 200 and r.headers["Content-Type"] == "text/html"
  end

  def self.test_default
    r = TestHTTP::get("http://localhost/default.txt")
    r.code == 200 and r.data.strip == "Test for `Default` rule" and r.headers["Server"] == self.get_vsn and r.headers["Content-Type"] == "text/plain" and r.headers["X-Powered-By"] == "Pusheen the Cat"
  end
end
