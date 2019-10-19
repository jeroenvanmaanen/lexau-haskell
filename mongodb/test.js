/* test.js -- test mongo */

// db.nodes.find().forEach(printjson);

var path = ["'", 'go', "'", ',', ' ', 'and'];

function pathToKey(path, x, index, max_index) {
    var asKey = (x == null ? true : x);
    //printjson({ x : x, asKey : asKey });
    var i = index || 0;
    var m = max_index || (path.length - 1);
    var result = '';
    if (i <= m) {
        var last = protect(path[i]);
        if (i < m) {
            tailKey = pathToKey(path, x, i + 1, m);
            if (asKey) {
                result = tailKey + ', ' + last;
            } else {
                result = last + ', ' + tailKey;
            }
        } else {
            result = last;
        }
    }
    return result;
}

function protect(str) {
    return "'" + str.replace("'", "''") + "'";
}

var node = {
    path : path,
    depth : path.length,
    key : pathToKey(path),
    observed : {
        "they" : 12,
        "it" : 3
    },
    expected : {
        "they" : 4,
        "it" : 1
    },
    delta : {
        "they" : 3,
        "it" : 1
    },
    codeLength : {
        "node" : 48,
        "subtree" : 318
    },
    updated : true
};
db.nodes.update({key : node.key}, node, true);
db.nodes.ensureIndex({key : 1}, {unique : true});


function ensureNode(path) {
    var node = {
        path : path,
        depth : path.length,
        key : pathToKey(path),
        observed : {},
        expected : {},
        delta : {},
        codeLength : {
            node : 1,
            subtree : 1
        },
        updated : false
    };
    db.nodes.insert(node);
}
var rootPath = [];
ensureNode(rootPath);
var rootKey = pathToKey(rootPath);
db.nodes.update(
    {
        key : rootKey,
        maxKeyLength : { $exists : false }
    },
    {
        $set : { maxKeyLength : rootKey.length }
    }
);

function getMaxKeyLength() {
    return db.nodes.findOne({ key : rootKey }, { maxKeyLength : 1 }).maxKeyLength;
}

function ensurePath(path) {
    var keyLength = pathToKey(path).length;
    var oldMaxKeyLength = getMaxKeyLength();
    if (keyLength > oldMaxKeyLength) {
        db.nodes.update(
            { key : rootKey },
            { $set : { maxKeyLength : keyLength } }
        );
    }
    var p = path.slice(0);
    while (p.length > 0) {
        ensureNode(p);
        p.shift();
    }
}

// Insert example data
function insertData(data) {
    path = data.slice(0);
    var p = [];
    while (path.length > 0) {
        w = path.shift();
        p.push(w);
        ensurePath(p);
    }
}
insertData(path);
insertData(["this", " ", "and", " ", "that"]);
insertData(["to", " ", "go", " ", "or", " ", "not", " ", "to", " ", "go"]);

function getPadding() {
    var maxKeyLength = getMaxKeyLength();
    var padding = '    ';
    while (padding.length < maxKeyLength) {
        padding = '' + padding + padding;
    }
    var offset = padding.length - maxKeyLength;
    printjson({ maxKeyLength : maxKeyLength, paddingA : padding, paddingLength : padding.length, offset : offset });
    var q = padding.substring(0, maxKeyLength);
    printjson({ maxKeyLength : maxKeyLength, paddingB : q });
    return q;
}
function padKey(padding, key) {
    return padding.substring(key.length) + key;
}
function paddedKey(padding, node) {
    return padKey(padding, pathToKey(node.path, false));
}

db.nodes.find().limit(4).forEach(printjson);
var padding = getPadding();
db.nodes.find().sort({key:1}).forEach(function(d){print('[', paddedKey(padding, d), ']:', d.depth, ':', d.key.length);});

function unregex(str) {
    return str.replace(new RegExp('\[|\]|[?+*()|]', 'g'), function(m) { return '\\' + m; });
}

print(unregex('star:*, braces:(), bar: |, plus: +, question: ?'));

function findChildren(path) {
    return db.nodes.find({ key : { $regex : '^' + unregex(pathToKey(path)) }, depth : path.length + 1 });
}
findChildren([' ', 'and']).forEach(function(d){print('[', paddedKey(padding, d), ']:', d.depth, ':', d.key.length);});

/* EOF */
