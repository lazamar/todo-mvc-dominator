exports.setStorage = function(state) {
    return function() {
        localStorage.setItem("dominator-todo-save", JSON.stringify(state));
    };
};

exports.focusElement = function(id) {
    return function() {
        var el = document.getElementById(id);
        if (el) {
            el.focus();
        }
    };
};
