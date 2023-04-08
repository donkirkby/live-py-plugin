import {compareCanvases, slicePixel} from './App';

it('compares two canvases', () => {
  const liveCanvas = document.createElement('canvas'),
      goalCanvas = document.createElement('canvas'),
      diffCanvas = document.createElement('canvas'),
      backgroundColour = '#ffffff';
  for (const canvas of [liveCanvas, goalCanvas, diffCanvas]) {
    canvas.height = 300;
    canvas.width = 400;
  }
  const liveCtx = liveCanvas.getContext('2d');
  liveCtx.fillStyle = backgroundColour;
  liveCtx.fillRect(0, 0, 400, 300);
  liveCtx.fillStyle = '#000096'; // 150 dark blue
  liveCtx.fillRect(0, 0, 100, 100);
  liveCtx.fillStyle = '#0000c8'; // 200 medium blue
  liveCtx.fillRect(100, 0, 100, 100);
  liveCtx.fillStyle = '#0000fa'; // 250 bright blue
  liveCtx.fillRect(200, 0, 100, 100);
  const goalCtx = goalCanvas.getContext('2d');
  goalCtx.fillStyle = backgroundColour;
  goalCtx.fillRect(0, 0, 400, 300);
  goalCtx.fillStyle = '#0000c8'; // 200 medium blue
  goalCtx.fillRect(0, 0, 300, 100);
  const foregroundPixels = 300*100,
      matchingPixels = 100*100,
      expectedProgress = 100 * matchingPixels / foregroundPixels;


  const progress = compareCanvases(
      liveCanvas,
      goalCanvas,
      diffCanvas,
      backgroundColour);

  expect(progress).toBeCloseTo(expectedProgress);
  const diffData = diffCanvas.getContext('2d').getImageData(
      0, 0,
      400, 300);
  expect(Array.from(slicePixel(diffData, 50, 50))).toEqual(
      [255, 0, Math.floor((150+200)/5), 255]); // dark diff
  expect(Array.from(slicePixel(diffData, 150, 50))).toEqual(
      [0, 0, 198, Math.floor(255/3)]); // medium match: why 198 instead of 200?
  expect(Array.from(slicePixel(diffData, 250, 50))).toEqual(
      [255, 255, Math.floor((250+200)/5), 255]); // bright diff
});
