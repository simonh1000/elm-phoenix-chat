defmodule Meepg.UserChannel do
  use Phoenix.Channel

  intercept ["send_sound"]

  def join("user:"<>username, _params, socket) do
    {:ok, socket}
  end

  # def send_sound(%{username: tgt, body: sf}, socket) do
  #     broadcast! socket, "new_msg", %{body: body, username: socket.assigns.username}
  # end

  # def handle_in("new_msg", %{"body" => body}, socket) do
  #     Meepg.broadcast! socket, "new_msg", %{body: body, username: socket.assigns.username}
  #     {:noreply, socket}
  #   end

  # def handle_in("send_sound", %{"target" => target}, socket) do
  #     IO.inspect("send_sound: " <> target)
  #   #   join ("sounds:" <> target), %{}, socket)
  #   #   IO.inspect(socket.assigns[:username])
  #     broadcast ("sounds:" <> target), "play_sound", %{}
  #     {:noreply, socket}
  #   end
  #
    def handle_out("send_sound", payload, socket) do
      push socket, "receive_sound", payload
      {:noreply, socket}
    end

end
