type Coordinates = { X : int ; Y: int; Z:int}
type Dim =  { Width : int ; Height:int ; Length :int}
type Container = { Dim : Dim; Coord: Coordinates}
type Item = { Dim : Dim}
type PutConfig = NA
type ContainerTriplet = Container list
type ItemPut = { Item : Item ; Coord : Coordinates}
type PutResult  = (ContainerTriplet list * ItemPut) option

type PutItem = Container -> Item  -> PutResult 
type StackItem = (Container list * Item list)
type CalculateCost = 
    PutItem 
        -> PutConfig list
        -> Container list 
        -> Item list 
        -> Item list

let putItem : PutItem =
    fun container item ->
        let remainingWidth = 
            container.Dim.Width - item.Dim.Width 
        let remainingHeight = 
            container.Dim.Height - item.Dim.Height
        let remainingLength =
            container.Dim.Length - item.Dim.Length

        if (remainingHeight < 0 ) || remainingLength < 0 || remainingWidth < 0 then
            None
        else
            let topBlock = {
                Dim = { Width = container.Dim.Width; Height = remainingHeight; Length = item.Dim.Length}
                Coord = {X = container.Coord.X ; Y= container.Coord.Y + item.Dim.Height; Z =container.Coord.Z}}
            
            let sideBlock = {
                Dim = { Width = remainingWidth; Height = item.Dim.Height; Length = item.Dim.Length}
                Coord = { X = container.Coord.X + item.Dim.Width ; Y=  container.Coord.Y; Z = container.Coord.Z}}

            let remainingBlock = {
                Dim = { Width = container.Dim.Width; Height = container.Dim.Height; Length = remainingLength}
                Coord = { X= container.Coord.X ; Y = container.Coord.Y; Z= container.Coord.Z + item.Dim.Length}}
            let itemPut = { Item = item; Coord = { X = container.Coord.X; Y = container.Coord.Y; Z = container.Coord.Z }}
            Some ([[topBlock;sideBlock;remainingBlock]], itemPut)


let calculateCost  =
    fun putItem
        containers 
        items -> 
            let rec loop : (StackItem list * ItemPut list) -> (StackItem list * ItemPut list)  =  function
                | (containerSet, item::remainingItems) :: remainingStack, itemsPut -> 
            //get a graph item from stack
           //get an item
                let rec loopContainers : Container list -> StackItem list * ItemPut option  = function
                    | container :: remainingContainers ->
                        match putItem container item with
                        | Some (containerTriplets, itemPut ) -> 
                            [
                                for triplet in containerTriplets do
                                    yield (remainingContainers @ triplet, remainingItems)
                            ], Some itemPut
                           
                        | _ -> loopContainers remainingContainers
                    | _ -> [], None
                let stackItems, itemPut = loopContainers containerSet
                let itemsPut = 
                    match itemPut with 
                    | Some i -> i :: itemsPut
                    | _ -> itemsPut
                     
                loop (stackItems @ remainingStack, itemsPut)
                |  _ , itemsPut-> [],itemsPut
            let init = [(containers,items)], []
           // loop init
            loop ([containers, items], [])


let contiainers = [{Dim = {Width = 100; Height = 100; Length = 10}; Coord = {X =0; Y =0; Z =0 }}]
let items = [{Dim = {Width = 20; Height = 20; Length = 10}};{Dim = {Width = 20; Height = 20; Length = 10}};{Dim = {Width = 20; Height = 20; Length = 10}}]

let _,res = calculateCost putItem contiainers items
printf "%A" res
           //pick a container from container set
           //try putting the item into container 
           //if fits then we get a container set of container triplets
           //  for each returned triplet 
           ///    merge it with remaining container set and merge it with remaining items
           ///    call above as graph_item
           //     put graph to stack
           // go back
           
           