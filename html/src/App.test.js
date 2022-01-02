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
  liveCtx.fillStyle = '#0000ff';
  liveCtx.fillRect(0, 0, 250, 200);
  const goalCtx = goalCanvas.getContext('2d');
  goalCtx.fillStyle = backgroundColour;
  goalCtx.fillRect(0, 0, 400, 300);
  goalCtx.fillStyle = '#00ff00';
  goalCtx.fillRect(150, 100, 250, 200);
  goalCtx.fillStyle = '#0000ff';
  goalCtx.fillRect(190, 140, 20, 20);
  const foregroundPixels = 250*200*2 - 100*100,
      matchingPixels = 20*20,
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
  expect(Array.from(slicePixel(diffData, 0, 0))).toEqual(
      [255, 51, 102, 255]);
  expect(Array.from(slicePixel(diffData, 399, 299))).toEqual(
      [255, 102, 51, 255]);
  expect(Array.from(slicePixel(diffData, 180, 130))).toEqual(
      [255, 51, 51, 255]);
  expect(Array.from(slicePixel(diffData, 200, 150))).toEqual(
      [0, 0, 255, 85]);
});
