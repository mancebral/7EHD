'use strict';

document.addEventListener('DOMContentLoaded', function() {
  const counterReset = document.querySelectorAll('.counter-reset');

  if (counterReset.length > 0) {

    counterReset.forEach((resetEl) => {
      resetEl.addEventListener('click', () => {
        let parent = resetEl.closest('div');
        let counter = parent.className;
        localStorage.removeItem('.' + counter);
        document.location.reload();
      });
    });
  }
});