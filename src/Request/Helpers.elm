module Request.Helpers exposing (apiUrl, loginUrl)


isDeployed : Bool
isDeployed =
    False


apiUrl : String -> String
apiUrl str =
    if isDeployed then
        "https://api.properties.leonti.me" ++ str
    else
        "http://localhost:9050" ++ str


loginUrl : String
loginUrl =
    if isDeployed then
        "https://leonti.au.auth0.com/authorize?client_id=ApfO999H1OvBttnoqtC3NUoCpTYAvht1&response_type=token%20id_token&redirect_uri=https%3A%2F%2Fproperties.leonti.me&scope=openid%20email&audience=rea-backend&state=s&nonce=n&auth0Client=eyJuYW1lIjoiYXV0aDAuanMiLCJ2ZXJzaW9uIjoiOC44LjAifQ%3D%3D"
    else
        "https://leonti.au.auth0.com/authorize?client_id=ApfO999H1OvBttnoqtC3NUoCpTYAvht1&response_type=token%20id_token&redirect_uri=http%3A%2F%2Flocalhost:8000&scope=openid%20email&audience=rea-backend&state=s&nonce=n&auth0Client=eyJuYW1lIjoiYXV0aDAuanMiLCJ2ZXJzaW9uIjoiOC44LjAifQ%3D%3D"
