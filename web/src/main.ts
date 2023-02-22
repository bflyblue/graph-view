import * as d3 from "d3";

type Node = d3.SimulationNodeDatum & {
  id: number;
};

type Edge = d3.SimulationLinkDatum<Node> & {};

type ApiGraph = ApiNode[];

type ApiNode = {
  node_id: number;
  labels: string[];
  properties: object;
  in: ApiEdge[];
  out: ApiEdge[];
};

type ApiEdge = {
  edge_id: number;
  labels: string[];
  properties: object;
  a: number;
  b: number;
};

const sourceNode = (d: Edge) => d.source as Node;
const targetNode = (d: Edge) => d.target as Node;

const graph = (nodes: Node[], edges: Edge[]) => {
  var tx = 0,
    ty = 0,
    s = 1,
    hover: Node | undefined = undefined;

  const canvas = d3.create("canvas");
  const ctx = canvas.node()!.getContext("2d")!;

  const observer = new ResizeObserver((_entries) => {
    const n = canvas.node()!;
    n.width = n.clientWidth;
    n.height = n.clientHeight;
  });
  observer.observe(canvas.node()!);

  ctx.strokeStyle = "black";
  ctx.lineWidth = 0.2;

  const drawLinks = () => {
    ctx.save();
    ctx.fillStyle = "transparent";
    ctx.lineWidth = 0.1;
    ctx.beginPath();
    edges.forEach((d) => {
      ctx.moveTo(sourceNode(d).x!, sourceNode(d).y!);
      ctx.lineTo(targetNode(d).x!, targetNode(d).y!);
    });
    ctx.stroke();
    ctx.restore();
  };

  const drawNodes = () => {
    ctx.save();
    ctx.font = "3px sans-serif";
    ctx.textAlign = "center";
    ctx.textBaseline = "middle";
    nodes.forEach((d, i) => {
      if (d === hover) {
        ctx.fillStyle = "red";
      } else {
        ctx.fillStyle = "green";
      }
      ctx.beginPath();
      ctx.arc(d.x!, d.y!, 5, 0, 2 * Math.PI);
      ctx.fill();
      ctx.stroke();
      ctx.fillStyle = "white";
      ctx.fillText(d.id.toString(), d.x!, d.y!);
    });
    ctx.restore();
  };

  const draw = () => {
    const { width, height } = canvas.node()!;
    ctx.resetTransform();
    ctx.clearRect(0, 0, width, height);
    ctx.translate(tx, ty);
    ctx.scale(s, s);
    ctx.translate(width / 2, height / 2);
    drawLinks();
    drawNodes();
    window.requestAnimationFrame(draw);
  };

  const simulation = d3
    .forceSimulation(nodes)
    .force(
      "link",
      d3.forceLink(edges).id((d) => (d as Node).id)
    )
    .force("charge", d3.forceManyBody())
    .force("x", d3.forceX())
    .force("y", d3.forceY());

  const zoom = d3
    .zoom<HTMLCanvasElement, undefined>()
    .scaleExtent([1, 8])
    .on("zoom", (event) => {
      tx = event.transform.x;
      ty = event.transform.y;
      s = event.transform.k;
    });

  canvas.on("mousedown", (event) => {
    const { width, height } = canvas.node()!;
    const p = d3.pointer(event, canvas.node());
    const l = [(p[0] - tx) / s - width / 2, (p[1] - ty) / s - height / 2];
    console.log(p, l);
    hover = simulation.find(l[0]!, l[1]!, 10);
  });

  canvas.call(zoom);

  draw();

  return canvas.node();
};

const main = async () => {
  const data: ApiGraph | undefined = await d3.json(
    "http://neptune:8080/api/graph"
  );
  if (data) {
    const div = d3.select("#graph");
    const nodes: Node[] = data.map((n) => ({ id: n.node_id }));
    const edges: Edge[] = data.flatMap((n) =>
      d3.map(n.in, (e) => ({ source: e.a, target: e.b }))
    );
    div.append(() => graph(nodes, edges));
  }
};

(async () => {
  try {
    await main();
  } catch (e) {
    console.log(e);
  }
})();
