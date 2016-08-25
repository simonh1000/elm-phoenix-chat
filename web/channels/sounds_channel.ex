defmodule Meepg.SoundsChannel do
  use Phoenix.Channel

  intercept ["new_msg"]

  def join("room:lobby", %{"username" => username}, socket) do
    # IO.inspect username
    send(self, :after_join)
    {:ok, socket |> assign(:username, username)}
  end

  def join("room:" <> _private_room_id, _params, _socket) do
    {:error, %{reason: "unauthorized"}}
  end

  def join("sounds:" <> username, _, socket) do
    IO.inspect("Creating channel sounds:" <> username)
    send(self, :after_join)
    {:ok, socket |> assign(:username, username)}
  end

  def terminate(_params, socket) do
      broadcast! socket, "lost_member", %{username: socket.assigns.username}
  end

  # Callback
  def handle_info(:after_join, socket) do
      broadcast! socket, "new_member", %{username: socket.assigns.username}
      {:noreply, socket}
  end

  def handle_in("new_msg", %{"body" => body}, socket) do
    #   IO.inspect("handle_in: " <> body)
    #   IO.inspect(socket.assigns[:username])
      Meepg.broadcast! socket, "new_msg", %{body: body, username: socket.assigns.username}
      {:noreply, socket}
    end

  def handle_in("send_sound", %{"target" => target}, socket) do
      IO.inspect("send_sound: " <> target)
    #   join ("sounds:" <> target), %{}, socket)
    #   IO.inspect(socket.assigns[:username])
      broadcast ("sounds:" <> target), "play_sound", %{}
      {:noreply, socket}
    end

    def handle_out("new_msg", payload, socket) do
      push socket, "new_msg", payload
      {:noreply, socket}
    end

end
