import asyncio
import time

async def tcp_echo_client(message, loop):
	# Connect to Welsh
	reader, writer = await asyncio.open_connection('127.0.0.1', 12000, loop=loop)
	# Connect to Holiday
	# reader, writer = await asyncio.open_connection('127.0.0.1', 11999, loop=loop)
	print("Sending:", message, end="")
	writer.write(message.encode())
	data = await reader.read(100000)
	print("Received:", data.decode(), end="")
	# writer.close()

def main():
	# message = "\t\t\t\t\t    \f\f\fIAMAT\v\v\v\v\v   \t\fkiwi.cs.ucla.edu -33.86705222+151.1957 {0}\f\r\f\f\t\t\r\n".format(time.time())
	# message = "IAMAT kiwi.cs.ucla.edu -32.12+152.2 {0}\n".format(time.time())
	# message = "IAMAT other +34.0698-118.445127 {0}\n".format(time.time())
	message = "WHATSAT kiwi.cs.ucla.edu 20 10\n"
	# message = "WHATSAT other 5 20\n"
	# message = "CHANGELOC kiwi.cs.ucla.edu -32.12+152.2 {0} {1} Holiday\n".format(time.time(), 2)
	loop = asyncio.get_event_loop()
	loop.run_until_complete(tcp_echo_client(message, loop))
	loop.close()

if __name__ == '__main__':
	main()

