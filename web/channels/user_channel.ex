defmodule EPChat.UserChannel do
  use Phoenix.Channel

  intercept ["send_sound"]

  def join("user:"<>username, _params, socket) do
    {:ok, socket |> assign(:username, username)}
  end

  def handle_in("send_sound", %{"target" => tgt, "soundfile" => sf}, socket) do
      IO.inspect("send_sound")
    #   broadcast! socket, "play_sound", %{username: tgt, body: sf}
      EPChat.Endpoint.broadcast("user:"<>tgt, "send_sound", %{body: sf, sender: socket.assigns[:username]})
      {:noreply, socket}
  end

  def handle_out("send_sound", payload, socket) do
    push socket, "receive_sound", payload
    {:noreply, socket}
  end

end
