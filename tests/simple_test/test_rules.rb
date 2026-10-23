require "testapi/http.rb"
require "socket"


module Tests
  # Sends one raw HTTP/1.1 request with "Connection: close" and returns the
  # exact bytes the server wrote, so a test can see what HTTParty hides
  # (Net::HTTP silently discards any body that follows a 204 or a HEAD).
  # Helpers must not start with "test_": SimpleTest runs every such method.
  def self.raw_http(method, path, host = "localhost", port = 80)
    sock = TCPSocket.new(host, port)
    sock.write("#{method} #{path} HTTP/1.1\r\nHost: #{host}\r\nConnection: close\r\n\r\n")
    sock.close_write
    data = sock.read
    sock.close
    head, _sep, body = data.partition("\r\n\r\n")
    status = head.lines.first.to_s.strip
    headers = head.lines.drop(1).map { |l| k, v = l.split(":", 2); [k.strip.downcase, v.to_s.strip] }.to_h
    [status, headers, body]
  end

  # A 204 must carry neither a body nor Content-Length (RFC 7230 3.3, 3.3.2).
  def self.bare_204?(status, headers, body)
    status.start_with?("HTTP/1.1 204") and body == "" and not headers.key?("content-length")
  end

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
    r.code == 200 and r.headers["X-Different-Host"] == "true" and r.headers["X-Powered-By"] == "Pusheen the Cat" \
      and r.headers["X-Continuation"] == "goose"
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

  # Send-File of an empty file answers 204 without Content-Length.
  # Regression: the server used to attach its HTML error page (145 bytes) and
  # its Content-Length to the 204; curl reported "Excess found writing body".
  def self.test_send_file_empty
    r = TestHTTP::get("http://localhost/send_empty")
    r.code == 204 and (r.data.nil? or r.data == "") and r.headers["content-length"].nil?
  end

  # Same check on the wire: nothing at all may follow the header terminator.
  def self.test_send_file_empty_wire
    status, headers, body = self.raw_http("GET", "/send_empty")
    self.bare_204?(status, headers, body)
  end

  # DocDir takes the other code path to the same 204 for an empty file.
  def self.test_docdir_empty
    status, headers, body = self.raw_http("GET", "/empty.txt")
    self.bare_204?(status, headers, body)
  end

  # HEAD gets the error headers of an Abort but never the HTML page (RFC 7231 4.3.2).
  def self.test_head_abort_no_body
    status, headers, body = self.raw_http("HEAD", "/i_am_a_teapot")
    status.start_with?("HTTP/1.1 418") and body == "" and headers["content-length"].to_i > 0
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
end
