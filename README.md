# poptape-auctioneer
Bidding microservice written in Erlang using Cowboy, RabbitMQ and ets.

This microservice is intended as a lightweight service for tracking bids on an 
auction and broadcast via RabbitMQ to all auction bidders. It uses the cowboy
webserver and RabbitMQ as well as ets tables.

#### The Process:

##### Auction Owner:
The auction owner sends an http post to an endpoint of `/auction/<auction_id>`
The post has the usual x-access-token JWT header as well as the following json
fields:
```
item\_id
username
start\_price
```
The microservice then creates an exchange on RabbitMQ of type 'fanout' with an id 
of item\_id. This type of exchange allows a pub/sub model to be used. The service 
also creates two queues; one with a name matching the item\_id and one matching 
the username. The item\_id queue is intended for an audit microservice to use and 
is saved to disk. The other queue is intended for use by the auction owner.

##### A Bidder:
Any bidders connect via a websocket to the cowboy webserver sending a JWT with the 
connection request. This is verified as valid by a call to the 'authy' authentication
microservice and, if valid, the JWT is stored in an ets table. Any subsequent 
websocket data sent checks against ets first. This is to enable faster running of the 
microservice and avoids too many http calls or disk accesses. Any data sent must contain
the x-access-token field with the JWT as the content of this field.
If the websocket connection is lost then the ets table entry is deleted and any 
subsequent connections need to be reverified against the authenication service.

The bidder comes in on an URL endpoint of `/auction/<auction_id>/<item_id>` and 
sends the data listed below in a Json format (as well as the x-access-token JWT) in the 
websocket body:
```
username
bid
```
The microservice creates a queue with the name of username (I think I need to change 
this to make each queue unique as a user can bid on more than one item at a time - not
 sure if RabbitMQ allows queues with non-unique names). 
The queue is bound to the exchange with the name that corresponds to the item\_id. 
Any subsequent websocket data sent is published to the exchange and all subscribers 
receive the messages to their own websocket.

Currently we return a whole load of data but this will undoubtably be reduced in the future 
to the bare minimum. 

#### Extra bits:
All queues and exchanges on on a RabbitMQ virtual host as set in the microservice config file.
Due to websockets, by default, having a timeout of 60 secs I've instigated a timer that sends
a 'ping' websocket frame back to the client/browser to keep the connection alive. Browsers
automatically respond with a pong websocket frame. 

Cowboy docs recommend that cowboy only respond to pings and not send pings as too many 
timers on the webserver could be a bad thing. However there doesn't exist a facility in the 
current javascript implementation of websockets to send a websocket 'ping' frame direct 
from javascript. Javascript docs say send a ping from the server and the server docs say 
the opposite!

### API routes

```
TBD
```

### Notes:
* Very early pre-alpha. Works(ish).

### TODO:
* Make bidder queue name unique on username and item\_id.
* Most of it!
