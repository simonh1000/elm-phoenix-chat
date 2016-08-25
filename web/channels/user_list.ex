defmodule Meepg.UserList do

    def start_link do
      IO.inspect("Starting agent")
      Agent.start_link(fn -> [] end, name: __MODULE__)
    end

    def join(user) do
        Agent.get_and_update(__MODULE__, fn userlist ->
            userlist2 = Enum.uniq [ user | userlist ]
          {userlist2, userlist2}
        end)
    end

    def leave(user) do
        Agent.update(__MODULE__, fn userlist ->
            Enum.filter(userlist, fn elem -> elem != user end)
        end)
    end
end
