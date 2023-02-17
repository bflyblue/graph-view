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

const graph = (nodes: Node[], edges: Edge[]) => {
  const svg = d3.create("svg").attr("viewBox", [-1000, -1000, 2000, 2000]);

  const link = svg
    .append("g")
    .attr("stroke", "#000")
    .attr("stroke-width", 1)
    .selectAll("line")
    .data(edges)
    .join("line");

  const node = svg
    .append("g")
    .attr("fill", "#272")
    .attr("stroke", "#000")
    .attr("stroke-width", 1)
    .selectAll("circle")
    .data(nodes)
    .join("circle")
    .attr("r", 5);

  const text = svg
    .append("g")
    .attr("text-anchor", "middle")
    .attr("font-size", 5)
    .selectAll("text")
    .data(nodes)
    .join("text")
    .text((d) => d.id);

  const ticked = () => {
    link
      .attr("x1", (d) => ((d! as Edge).source! as Node).x!)
      .attr("y1", (d) => ((d! as Edge).source! as Node).y!)
      .attr("x2", (d) => ((d! as Edge).target! as Node).x!)
      .attr("y2", (d) => ((d! as Edge).target! as Node).y!);
    node.attr("cx", (d) => (d! as Node).x!).attr("cy", (d) => (d! as Node).y!);
    text
      .attr("x", (d) => (d! as Node).x!)
      .attr("y", (d) => (d! as Node).y! + 2);
  };

  const zooming = d3
    .zoom<SVGSVGElement, undefined>()
    .scaleExtent([1, 8])
    .on("zoom", (event) => {
      link
        .attr("transform", event.transform)
        .attr("stroke-width", 1 / event.transform.k);
      node
        .attr("transform", event.transform)
        .attr("stroke-width", 1 / Math.sqrt(event.transform.k));
      text.attr("transform", event.transform);
    });

  const drag = (simulation: d3.Simulation<Node, Edge>) => {
    const dragstarted = (event: any) => {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      event.subject.fx = event.subject.x;
      event.subject.fy = event.subject.y;
    };

    const dragged = (event: any) => {
      event.subject.fx = event.x;
      event.subject.fy = event.y;
    };

    const dragended = (event: any) => {
      if (!event.active) simulation.alphaTarget(0);
      event.subject.fx = null;
      event.subject.fy = null;
    };

    return d3
      .drag<SVGSVGElement, undefined>()
      .on("start", dragstarted)
      .on("drag", dragged)
      .on("end", dragended);
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

  // svg.call(drag(simulation));
  svg.call(zooming);

  return svg.node();
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
