defmodule EPChat.RoomChannel do
  use Phoenix.Channel
  alias EPChat.UserList


  intercept ["new_msg"]

  def join("room:lobby", %{"username" => username}, socket) do
    # IO.inspect username
    send(self, :after_join)
    users = UserList.get_users()
    {:ok, %{users: users}, socket |> assign(:username, username)}
  end

  def join("room:" <> _private_room_id, _params, _socket) do
    {:error, %{reason: "unauthorized"}}
  end

  def terminate(_params, socket) do
      new_list = UserList.leave(socket.assigns.username)
      broadcast! socket, "lost_member", %{username: socket.assigns.username, users: new_list}
  end

  # Callback
  def handle_info(:after_join, socket) do
    #   IO.inspect(socket.assigns.username)
      new_list = UserList.join(socket.assigns.username)
      IO.inspect(new_list)
      broadcast! socket, "new_member", %{username: socket.assigns.username, users: new_list}
      {:noreply, socket}
  end

  def handle_in("new_msg", %{"body" => body}, socket) do
    #   IO.inspect("handle_in: " <> body)
    #   IO.inspect(socket.assigns[:username])
      broadcast! socket, "new_msg", %{username: socket.assigns.username, body: body}
      {:noreply, socket}
    end

    def handle_out("new_msg", payload, socket) do
      push socket, "new_msg", payload
      {:noreply, socket}
    end

end
