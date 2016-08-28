defmodule EPChat.DefaultController do
  use EPChat.Web, :controller

  def show(conn, _params) do
    json conn, %{data: "Hello, World!"}
  end
end
