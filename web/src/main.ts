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
  const width = 2000,
    height = 2000;

  var tx = 0,
    ty = 0,
    s = 1;

  const canvas = d3
    .create("canvas")
    .attr("width", width)
    .attr("height", height);

  const ctx = canvas.node()!.getContext("2d")!;

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
      ctx.fillStyle = "green";
      ctx.beginPath();
      ctx.arc(d.x!, d.y!, 5, 0, 2 * Math.PI);
      ctx.fill();
      ctx.stroke();
      ctx.fillStyle = "white";
      ctx.fillText(d.id.toString(), d.x!, d.y!);
    });
    ctx.restore();
  };

  const ticked = () => {
    ctx.resetTransform();
    ctx.clearRect(0, 0, width, height);
    ctx.translate(tx, ty);
    ctx.scale(s, s);
    ctx.translate(width / 2, height / 2);
    drawLinks();
    drawNodes();
  };

  const simulation = d3
    .forceSimulation(nodes)
    .force(
      "link",
      d3.forceLink(edges).id((d) => (d as Node).id)
    )
    .force("charge", d3.forceManyBody())
    .force("x", d3.forceX())
    .force("y", d3.forceY())
    .on("tick", ticked);

  const zoom = d3
    .zoom<HTMLCanvasElement, undefined>()
    .scaleExtent([1, 8])
    .on("zoom", (event) => {
      tx = event.transform.x;
      ty = event.transform.y;
      s = event.transform.k;
      ticked();
    });

  canvas.call(zoom);

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
