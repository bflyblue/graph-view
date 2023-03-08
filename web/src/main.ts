import * as d3 from "d3";

type Node = d3.SimulationNodeDatum & {
  id: number;
  node: ApiNode;
  weight: number;
  size: number;
};

type Edge = d3.SimulationLinkDatum<Node> & {
  edge: ApiEdge;
};

type ApiGraph = ApiNode[];

type Tag = {
  key: string;
  value: string;
};

type ApiNode = {
  node_id: number;
  labels: string[];
  properties: object;
  in: ApiEdge[];
  out: ApiEdge[];
  tags: Tag[];
};

type ApiEdge = {
  edge_id: number;
  labels: string[];
  properties: object;
  a: number;
  b: number;
};

const lookup = (tags: Tag[], needle: string): string | undefined => {
  return tags.find((t) => t.key === needle)?.value;
};

const nodeLabel = (n: Node): string | undefined => {
  return (
    lookup(n.node.tags, "Name") || (n.node.properties as any)["functionName"]
  );
};

const sourceNode = (d: Edge) => d.source as Node;
const targetNode = (d: Edge) => d.target as Node;

const graph = (nodes: Node[], edges: Edge[]) => {
  var tx = 0,
    ty = 0,
    s = 1;

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
    ctx.font = '3px "Roboto Condensed", serif';
    ctx.textAlign = "center";
    ctx.textBaseline = "middle";
    nodes.forEach((d, _i) => {
      if (d === selectedNode) {
        ctx.fillStyle = "black";
      } else {
        if (selectedNode === undefined) {
          ctx.fillStyle = "green";
        } else {
          if (d.node.in.some((e) => e.a === selectedNode?.id)) {
            ctx.fillStyle = "blue";
          } else if (d.node.out.some((e) => e.b === selectedNode?.id)) {
            ctx.fillStyle = "red";
          } else {
            ctx.fillStyle = "grey";
          }
        }
      }
      ctx.beginPath();
      ctx.arc(d.x!, d.y!, d.size, 0, 2 * Math.PI);
      ctx.fill();
      ctx.stroke();
      if (s > 2) {
        ctx.fillStyle = "white";
        ctx.fillText(d.id.toString(), d.x!, d.y! + 0.3);
      }
    });
    ctx.restore();
  };

  const drawHover = () => {
    if (hoverNode && hoverNode.x !== undefined && hoverNode.y !== undefined) {
      const pad = 2;

      ctx.save();
      ctx.font = '16px "Roboto Condensed", serif';
      ctx.textAlign = "start";
      ctx.textBaseline = "top";

      const name = nodeLabel(hoverNode) || hoverNode.id.toString();
      const type = hoverNode.node.labels.toString();
      const width = Math.max(
        ctx.measureText(name).width,
        ctx.measureText(type).width
      );

      ctx.translate(hoverNode.x, hoverNode.y);
      ctx.translate(5, -5);
      ctx.scale(1 / s, 1 / s);

      ctx.fillStyle = "white";
      ctx.strokeStyle = "black";

      ctx.lineWidth = 0.5;
      ctx.beginPath();
      ctx.lineTo(-pad, -pad - 32);
      ctx.lineTo(width + pad, -pad - 32);
      ctx.lineTo(width + pad, pad);
      ctx.lineTo(-pad, pad);
      ctx.closePath();
      ctx.fill();
      ctx.stroke();

      ctx.fillStyle = "black";
      ctx.fillText(name, 0, -32);
      ctx.fillText(type, 0, -14);
      ctx.restore();
    }
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
    drawHover();
    window.requestAnimationFrame(draw);
  };

  const simulation = d3
    .forceSimulation(nodes)
    .alphaDecay(0.01)
    .force(
      "link",
      d3.forceLink(edges).id((d) => (d as Node).id)
    )
    .force(
      "charge",
      d3.forceManyBody().strength((n) => (n as Node).weight * -5 - 20)
    )
    .force("x", d3.forceX())
    .force("y", d3.forceY())
    .on("tick", () => {
      findHover();
    });

  const zoom = d3
    .zoom<HTMLCanvasElement, undefined>()
    .scaleExtent([0.2, 8])
    .on("zoom", (event) => {
      tx = event.transform.x;
      ty = event.transform.y;
      s = event.transform.k;
    });

  var selectedNode: Node | undefined = undefined;

  const selectNode = async (node: Node | undefined) => {
    selectedNode = node;
    const details = d3.select("div#details");

    // const node: ApiNode | undefined =
    //   id === undefined
    //     ? undefined
    //     : await d3.json(`http://neptune:8080/api/node/${id}`);

    details.selectChildren().remove();
    if (node) {
      const n = details.append("div").classed("node", true);
      n.append("div").classed("id", true).html(node.id.toString());
      n.append("div")
        .classed("labels", true)
        .selectAll("div")
        .data(node.node.labels)
        .join("div")
        .classed("label", true)
        .html((l) => l);
      const p = n.append("div").classed("properties", true);
      appendProps(p, node.node.properties);
    }
  };

  var hoverNode: Node | undefined = undefined;
  var hoverPos: { x: number; y: number } | undefined = undefined;

  const distance = (
    a: { x: number; y: number },
    b: { x: number; y: number }
  ): number => {
    const dx = a.x - b.x;
    const dy = a.y - b.y;
    return Math.sqrt(dx * dx + dy * dy);
  };

  const findHover = async () => {
    if (hoverPos) {
      var searchRadius = 100;
      hoverNode = undefined;
      do {
        if (hoverNode !== undefined) {
          searchRadius = hoverNode.size * 0.99;
        }
        hoverNode = simulation.find(hoverPos.x, hoverPos.y, searchRadius);
      } while (
        hoverNode !== undefined &&
        distance(hoverPos, hoverNode as { x: number; y: number }) >
          hoverNode.size
      );
    } else {
      hoverNode = undefined;
    }
  };

  const appendProps = (
    element: d3.Selection<any, unknown, HTMLElement, unknown>,
    p: any
  ) => {
    const v = element.append("div").classed("value", true);
    if (p === null) {
      v.classed("null", true).html("null");
    } else if (typeof p === "string") {
      v.classed("string", true).html(p);
    } else if (typeof p === "symbol") {
      v.classed("string", true).html(p.toString());
    } else if (typeof p === "number" || typeof p === "bigint") {
      v.classed("number", true).html(p.toString());
    } else if (typeof p === "boolean") {
      v.classed("bool", true).html(p.toString());
    } else if (Array.isArray(p)) {
      const a = v.classed("array", true);
      for (const k in p) {
        appendProps(a, p[k]);
      }
    } else if (typeof p === "object") {
      const o = v.classed("object", true);
      for (const k in p) {
        o.append("div").classed("key", true).html(k);
        const v = o.append("div").classed("val", true);
        appendProps(v, p[k]);
      }
    }
  };

  canvas.on("click", async (event) => {
    if (event.detail === 1) {
      const { width, height } = canvas.node()!;
      const p = d3.pointer(event, canvas.node());
      const l = [(p[0] - tx) / s - width / 2, (p[1] - ty) / s - height / 2];
      const clicked = simulation.find(l[0]!, l[1]!, 5);
      await selectNode(clicked);
    }
  });

  canvas.on("mousemove", async (event) => {
    const { width, height } = canvas.node()!;
    const p = d3.pointer(event, canvas.node());
    hoverPos = {
      x: (p[0] - tx) / s - width / 2,
      y: (p[1] - ty) / s - height / 2,
    };
    findHover();
  });

  canvas.call(zoom);

  draw();

  return canvas.node();
};

const node_weight = (n: ApiNode): number => {
  return n.in.length + n.out.length;
};

const node_size = (n: ApiNode): number => {
  return 5 + 0.05 * node_weight(n);
};

const main = async () => {
  const data: ApiGraph | undefined = await d3.json(
    "http://neptune:8080/api/graph"
  );
  if (data) {
    const div = d3.select("#graph");
    const nodes: Node[] = data.map((n) => ({
      id: n.node_id,
      node: n,
      weight: node_weight(n),
      size: node_size(n),
    }));
    const edges: Edge[] = data.flatMap((n) =>
      d3.map(n.in, (e) => ({ source: e.a, target: e.b, edge: e }))
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
