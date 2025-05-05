const imageCount = 74;
const imageFolder = 'images/field/';
const imagePrefix = 'IMG';
const imageExt = '.jpeg';

// Create an array of image paths
let images = [];
for (let i = 1; i <= imageCount; i++) {
    const paddedNumber = String(i).padStart(4, '0'); // e.g., "0001"
    images.push(`${imageFolder}${imagePrefix}${"_"}${paddedNumber}${imageExt}`);
}

// Shuffle the array
function shuffle(array) {
  for (let i = array.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [array[i], array[j]] = [array[j], array[i]];
  }
}
shuffle(images);

// Cycle through images
let current = 0;
const imgElement = document.getElementById('slideshow');

function showNextImage() {
  imgElement.src = images[current];
  current = (current + 1) % images.length;
}
showNextImage();
setInterval(showNextImage, 2500); // Change every 3 seconds
