import Main from "./Main.elm"

class Diagram extends HTMLElement {
    elmApp = undefined;
    // called when the element is added to the DOM
    connectedCallback() {
        const contentElement = document.createElement("div");
        this.appendChild(contentElement)
        this.elmApp = Main.init({
            node: contentElement,
            flags: this.dataset
        })
    }
}

customElements.define("diagram-over-time", Diagram);
