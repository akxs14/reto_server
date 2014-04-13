curl -X POST -H "Content-Type: application/json" -d '{
    "id": "1234534625254",
    "at": 2,
    "tmax": 120,
    "imp": [
        {
            "id": "1",
            "banner": {
                "w": 300,
                "h": 250,
                "pos": 1,
                "battr": [
                    13
                ]
            }
        }
    ],
    "badv" : [
      "company1.com",
      "company2.com"
    ],    
    "site": {
        "id": "234563",
        "name": "Site ABCD",
        "domain": "siteabcd.com",
        "cat": [
            "IAB2-1",
            "IAB2-2"
        ],
        "privacypolicy": 1,
        "page": "http://siteabcd.com/page.htm",
        "ref": "http://referringsite.com/referringpage.htm",
        "publisher": {
            "id": "pub12345",
            "name": "Publisher A"
        },
        "content": {
            "keywords": "keyword a, keyword b, keyword c"
        }
    },
    "device": {
        "ip": "64.124.253.1",
        "ua": "bla bla bla",
        "os": "OS X",
        "flashver": "10.1",
        "js": 1
    },
    "user": {
        "id": "45asdf987656789adfad4678rew656789",
        "buyeruid": "5df678asd8987656asdf78987654"
    }
}' http://localhost:8080/openrtb2
