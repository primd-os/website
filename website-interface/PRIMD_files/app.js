setInterval(() => {
    document.getElementById("scrollArrow").style.opacity = 1-window.scrollY/100
}, 20)

function scrollDown() {
    window.scrollTo({
        top: window.innerHeight+1,
        behavior: "smooth"
    })
}

function toggleModal(elem) {
    elem.getElementsByClassName("modal")[0].classList.toggle("is-active")
}