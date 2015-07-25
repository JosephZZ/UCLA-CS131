from twisted.internet.protocol import Factory, ClientFactory
from twisted.protocols.basic import LineReceiver
from twisted.application import service, internet
from twisted.internet import reactor
import time, datetime, urllib, json, logging, re, sys

PORTS = {
    "Alford"   : 13580,
    "Bolden"   : 13581,
    "Parker"   : 13582,
    "Powell"   : 13583,
    "Hamilton" : 13584
}

SERVER_HERD = {
    "Alford"   : ["Parker", "Powell"],
    "Parker"   : ["Alford", "Bolden", "Hamilton"],
    "Powell"   : ["Alford", "Bolden"],
    "Bolden"   : ["Parker", "Powell"],
    "Hamilton" : ["Parker"]
}

class Server(LineReceiver):
    def __init__(self, factory):
        self.factory = factory
        self.name = self.factory.name

    def connectionMade(self):
        self.factory.num_connections +=  1
        self.sendLine("hey wus up bro")
        logging.info("Connection with {0} established. Total: {1}".format(
            self.name, self.factory.num_connections))
        #print self.name+": connection made with %s total connections" % self.factory.num_connections

    def connectionLost(self, reason):
        self.factory.num_connections -= 1
        logging.info("Connection with {0} lost. Total: {1}".format(
            self.name, self.factory.num_connections))
        #print self.name+": connection lost with %s remaining connections" % self.factory.num_connections

    def lineReceived(self, line):
        logging.info("Input received: {0}".format(line))
        if line[0:6] == "IAMAT ":
            self.handle_IAMAT(line[6:])
        elif line[0:8] == "WHATSAT ":
            self.handle_WHATSAT(line[8:])
        elif line[0:3] == "AT ":
            self.handle_AT(line[3:])
        else:
            self.invalidate_request(line)

    def handle_AT(self, line):
        message = line.split()
        time_stamp = float(message[2])
        #if the location update time is not the newest, update it and keep propagate
        #else stop propagating
        #this is a very simple way equivalent of implementing a minimal spanning tree that has a slight overhead
        try:
            last_update = self.factory.users[message[0]]['update_time']
            if time_stamp > last_update: 
                self.update_location(message)
                self.propagate_to_neighbors(line)
                #print self.name+" received msg and keep propagating"
            #else:
                #print self.name+" stop propagating"
        except KeyError:
            self.update_location(message)
            self.propagate_to_neighbors(line)
            #print self.name+" received msg and keep propagating"


    
    def handle_IAMAT(self, line):
        message = line.split()
        if len(message) != 3:
            self.invalidate_request("IAMAT "+line)
        else:
            self.update_location(message)
            self.propagate_to_neighbors(line)
            time_stamp = float(message[2])
            time_taken = time.time() - time_stamp
            if time_taken > 0.0:
                reply_message = "AT %s +%s %s" % (self.name, time_taken, line)
            else:
                reply_message = "AT %s %s %s" % (self.name, time_taken, line)
            self.sendLine(reply_message)
            logging.info("Reply to IAMAT request: {0}".format(reply_message))

    def handle_WHATSAT(self, line):
        message = self.parse_WHATSAT(line)
        if message == False:
            self.invalidate_request("WHATSAT "+line)
        else:
            try:
                LOCATION = self.factory.users[message['user']]['location']
            except KeyError:
                self.invalidate_request("WHATSAT "+line)
            RADIUS = message['radius']
            AUTH_KEY = "AIzaSyDlBNq0HlXBWo1MUDMkTjij_bPIg5HUBqE"
            MyUrl = ('https://maps.googleapis.com/maps/api/place/nearbysearch/json'
                   '?location=%s'
                   '&radius=%s'
                   '&key=%s') % (LOCATION, RADIUS, AUTH_KEY)
            #print MyUrl
            response = urllib.urlopen(MyUrl)
            jsonStr = response.read()
            jsonData = json.loads(jsonStr)
            if len(jsonData['results']) > message['quantity']:
                jsonData['results'] = jsonData['results'][0:message['quantity']]
                jsonStr = json.dumps(jsonData, indent = 3, sort_keys=True)

            time_taken = time.time()-self.factory.users[message['user']]['update_time']
            reply_header = "AT %s +%s %s\n" % (self.name, time_taken, line)
            reply_message = reply_header+jsonStr
            self.sendLine(reply_message)
            logging.info("Reply to WHATSAT request: {0}".format(reply_message))

    def invalidate_request(self,line):
        self.sendLine("? "+line)
        logging.error("Invalid request: {0}".format(line))

    def update_location(self,msg):
        location = msg[1][:10]+","+msg[1][10:]
        self.factory.users[msg[0]]={'location':location, 'update_time':float(msg[2])}
        #print self.factory.users
        return msg

    def propagate_to_neighbors(self,line):
        for server in SERVER_HERD[self.name]:
                port = PORTS[server]
                reactor.connectTCP('localhost',port,ClientFactory("AT "+line))
        logging.info("Sent message: {0} to the following servers {1}".format("AT "+line, str(SERVER_HERD[self.name])))

    def parse_WHATSAT(self,line):
        msg = line.split()
        if len(msg) != 3:
            return False
        else:
            radius = int(msg[1])
            quantity = int(msg[2])
            if radius>50:
                return False
            elif quantity>20:
                return False
            else:
                return {'user':msg[0],'radius':msg[1],'quantity':quantity}



class ClientProtocol(LineReceiver):
    def __init__(self, factory):
        self.factory = factory

    def connectionMade(self):
        
        self.factory.num_connections +=  1
        self.sendLine(self.factory.line)
        #print " connection made with %s total connections" % self.factory.num_connections
        self.transport.loseConnection()

    def connectionLost(self, reason):
        self.factory.num_connections -=  1
        #print " connection lost with %s remaining connections" % self.factory.num_connections


class ClientFactory(ClientFactory):
    def __init__(self, line):
        self.line = line
        self.num_connections = 0

    def buildProtocol(self, addr):
        return ClientProtocol(self)

class ServerFactory(Factory):
    def __init__(self, name):
        self.name = name
        self.num_connections = 0
        self.users = {}
        filename = self.name + "_" + re.sub(r'[:T]', '_', datetime.datetime.utcnow().isoformat().split('.')[0]) + ".log"
        logging.basicConfig(filename = filename, level=logging.DEBUG)
        logging.info('{0} server started'.format(self.name))

    def buildProtocol(self, addr):
        return Server(self)

    def stopFactory(self):
        logging.info("{0} server shutdown".format(self.name))


def main():
    if len(sys.argv) != 2:
        print "Error: incorrect number of arguments"
        exit()
    factory = ServerFactory(sys.argv[1])

    reactor.listenTCP(PORTS[sys.argv[1]], factory)
    reactor.run()

if __name__ == '__main__':
    main()