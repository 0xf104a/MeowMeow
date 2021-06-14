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
end
