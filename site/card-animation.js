// Decorative forecast illustrations shared by the live view and sandbox.
let scene;

export function updateForecastVisuals(expectedCatch, mistPercent) {
  if (!scene) scene = createScene();
  scene.update(expectedCatch, mistPercent);
}

function createScene() {
  const cards = [document.querySelector(".today-card"), document.querySelector(".mist-signal")];
  const layers = cards.map((card, i) => {
    const canvas = document.createElement("canvas");
    canvas.className = `forecast-animation ${i ? "mist-animation" : "bird-animation"}`;
    canvas.setAttribute("aria-hidden", "true");
    card.prepend(canvas);
    return { canvas, context: canvas.getContext("2d"), width: 0, height: 0, visible: true };
  });
  const birds = [];
  const clouds = Array.from({ length: 16 }, (_, i) => ({
    x: (i * 0.61803398875) % 1, y: (i * 0.38196601125) % 1,
    radius: 0.22 + (i % 4) * 0.06, phase: i * 2.4
  }));
  const motion = matchMedia("(prefers-reduced-motion: reduce)");
  let mist = 0, targetMist = 0, targetCount = 0, frame = null, previousTime = null, time = 0;

  function draw(timestamp) {
    frame = null;
    const delta = previousTime == null ? 0 : Math.min(0.05, (timestamp - previousTime) / 1000);
    previousTime = timestamp;
    if (!motion.matches) time += delta;
    const blend = motion.matches ? 1 : 1 - Math.exp(-delta * 5);
    mist += (targetMist - mist) * blend;
    birds.forEach((bird, i) => { bird.level += ((i < targetCount ? 1 : 0) - bird.level) * blend; });
    while (birds.length > targetCount && birds.at(-1).level < 0.003) birds.pop();
    layers.forEach(({ context, width, height, visible }, layer) => {
      context.clearRect(0, 0, width, height);
      if (!visible || width === 0 || height === 0) return;
      if (layer === 0) {
        birds.forEach(bird => {
          if (!motion.matches) bird.y = (bird.y + delta * bird.speed / height) % 1;
          const x = bird.x * width + Math.sin(time * 0.6 + bird.phase) * 6;
          const y = bird.y * height;
          context.strokeStyle = `rgba(255, 222, 148, ${bird.opacity * bird.level})`;
          context.lineWidth = bird.size * 0.7;
          context.lineCap = "round";
          context.beginPath();
          context.moveTo(x - bird.size, y - bird.size);
          context.lineTo(x, y);
          context.lineTo(x + bird.size, y - bird.size);
          context.stroke();
        });
      } else if (mist > 0) {
        clouds.forEach(cloud => {
          const radius = Math.max(width, height) * cloud.radius * (1 + 0.16 * Math.sin(time * 0.55 + cloud.phase));
          const x = (cloud.x + Math.sin(time * 0.42 + cloud.phase) * 0.26) * width;
          const y = (cloud.y + Math.cos(time * 0.31 + cloud.phase) * 0.18) * height;
          const strength = mist * (0.78 + 0.22 * Math.sin(time * 0.8 + cloud.phase));
          const fog = context.createRadialGradient(x, y, 0, x, y, radius);
          fog.addColorStop(0, `rgba(235, 239, 240, ${strength * 0.17})`);
          fog.addColorStop(0.45, `rgba(197, 211, 215, ${strength * 0.075})`);
          fog.addColorStop(1, "rgba(197, 211, 215, 0)");
          context.fillStyle = fog;
          context.fillRect(x - radius, y - radius, radius * 2, radius * 2);
        });
      }
    });
    if (!motion.matches && !document.hidden && (birds.length || targetMist > 0 || mist > 0.001) && layers.some(layer => layer.visible)) frame = requestAnimationFrame(draw);
  }

  function refresh() {
    if (frame !== null) cancelAnimationFrame(frame);
    previousTime = null;
    draw(performance.now());
  }
  new ResizeObserver(() => {
    layers.forEach(layer => {
      const bounds = layer.canvas.getBoundingClientRect();
      layer.width = bounds.width; layer.height = bounds.height;
      const ratio = Math.min(devicePixelRatio, 2);
      layer.canvas.width = Math.round(bounds.width * ratio);
      layer.canvas.height = Math.round(bounds.height * ratio);
      layer.context.setTransform(ratio, 0, 0, ratio, 0, 0);
    });
    refresh();
  }).observe(document.querySelector(".live-cards"));
  const visibility = new IntersectionObserver(entries => {
    entries.forEach(entry => { layers[cards.indexOf(entry.target)].visible = entry.isIntersecting; });
    refresh();
  });
  cards.forEach(card => visibility.observe(card));
  motion.addEventListener("change", refresh);
  document.addEventListener("visibilitychange", refresh);

  return { update(expectedCatch, mistPercent) {
    // A modest density boost at high catches; existing birds retain their trajectories.
    targetCount = Math.min(2000, Math.round(expectedCatch / 10 * (1 + 0.25 * expectedCatch / (expectedCatch + 1000))));
    while (birds.length < targetCount) birds.push({ x: Math.random(), y: Math.random(), level: 0,
      speed: 20 + Math.random() * 24, size: 1.2 + Math.random() * 1.6,
      opacity: 0.18 + Math.random() * 0.3, phase: Math.random() * Math.PI * 2 });
    targetMist = mistPercent / 100;
    layers[0].canvas.dataset.particles = targetCount;
    layers[1].canvas.dataset.probability = mistPercent;
    // Weather drags only change targets, leaving the running animation clock intact.
    if (frame === null || motion.matches) refresh();
  } };
}
