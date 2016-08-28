defmodule EPChat.PageController do
  use EPChat.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
